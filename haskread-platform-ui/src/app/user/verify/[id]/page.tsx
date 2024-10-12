'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';

export default function UserVerify({ params }: { params: { id: number } }) {
  const [otp, setOtp] = useState('');
  const [error, setError] = useState('');
  const router = useRouter();

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError(''); // Clear previous errors

    try {
      const response = await fetch(`http://localhost:8085/api/v1/user/auth/verify/${params.id}/${otp}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      if (response.status === 200) {
        router.push('/'); // Redirect to the home page on success
      } else {
        const res = await response.text();
        setError(res || 'Failed to verify OTP');
      }
    } catch (error) {
      setError('An error occurred during verification: ' + error);
    }
  };

  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center 
                    min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <h1>Verify User ID {params.id}</h1>
      <form onSubmit={handleSubmit} className="flex flex-col items-center">
        <div className="mb-4">
          <label htmlFor="otp" className="block text-gray-700">Enter OTP</label>
          <input
            type="text"
            id="otp"
            value={otp}
            onChange={(e) => setOtp(e.target.value)}
            className="w-full px-3 py-2 border rounded"
            required
          />
        </div>
        <button
          type="submit"
          className="w-full bg-blue-500 text-white py-2 rounded hover:bg-blue-600"
        >
          Verify
        </button>
      </form>
      {error && <p className="text-red-500 text-sm mt-4">{error}</p>}
    </div>
  );
}
